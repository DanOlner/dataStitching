#We only have back to 1990, so can only cover 3 censuses
geolibs <- c("spdep","dplyr", "tidyr","assertthat","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster","readr")
lapply(geolibs, require, character.only = TRUE)

#general data reshaping and saving in different forms for different people at different times.

#5 census stitching: save as CSVs for Geoff.

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Random contig matrices
iz <- readOGR(dsn="C:/Data/MapPolygons/England/2011/England_lad_2011_gen_clipped", 
              layer="England_lad_2011_gen_clipped")

#spdep
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
contig <- poly2nb(iz, row.names = iz@data$NAME, snap = 250)

#Binary style, default is row-normalised
mx <- nb2mat(contig,zero.policy = T,style = 'B')

#OK. Weights add to 1 over rows.
apply(mx,1,sum)

write.csv(mx,"R_data/bits/England_2011LADs_spatialWeightsQueenContig_binary.csv")

#Check some
num <- 10
iz$NAME[num]
plot(iz[unlist(contig[num]),], col = 'green')
plot(iz[num,],add=T, col='grey')

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Contiguity matrix for 2011 IZs.----
#Various options...

iz <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
                     layer="scotland_ig_2011")

#spdep
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
contig <- poly2nb(iz, row.names = iz@data$interzone)

mx <- nb2mat(contig,zero.policy = T)

#OK. Weights add to 1 over rows.
apply(mx,1,sum)

write.csv(mx,"StitchOutputs/Scotland/other/Scots_IZs_spatialWeightsQueenContig.csv")

#Is this finding the centroids? Via
#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html
coords <- coordinates(iz)

#plot(iz)
#Yup, centroids
plot(iz, col='grey')
plot(contig,coords,col='red',lwd=2,add=TRUE)

contig_nk <- knn2nb(knearneigh(coords,k=4), row.names = iz@data$interzone)

#plot(iz, col='grey')
plot(contig_nk,coords,col='red',lwd=2,add=TRUE)

mx2 <- nb2mat(contig_nk,zero.policy = T)

write.csv(mx2,"StitchOutputs/Scotland/other/Scots_IZs_spatialWeights_4nearestNeighb.csv")

#~~~~~~~~~~~~~~~~~~~~~~
#Get weights matrices for 91 LBS adjusted postcode sectors
#Well, this has the correct zones in, right?
iz <- readOGR(dsn="C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth", 
              layer="1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch")

#spdep
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
contig <- poly2nb(iz, row.names = iz@data$label)

mx <- nb2mat(contig,zero.policy = T)

#OK. Weights add to 1 over rows. Except some that don't. No actual neighbours?
apply(mx,1,sum)

write.csv(mx,"StitchOutputs/Scotland/other/adjusted91PCS_spatialWeightsQueenContig.csv")

#Is this finding the centroids? Via
#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html
coords <- coordinates(iz)

#plot(iz)
#Yup, centroids
#plot(iz, col='grey')
#plot(contig,coords,col='red',lwd=2,add=TRUE)

contig_nk <- knn2nb(knearneigh(coords,k=8), row.names = iz@data$label)


#contig_nk_df <- contig_nk %>% data.frame()

#plot(iz, col='grey')
#xy=locator(2,"p") 
# plot(scotmap, xlim=xy$x,ylim=xy$y)
#plot(iz, col='grey', xlim=xy$x,ylim=xy$y)
#plot(contig_nk,coords,col='red',lwd=2,add=TRUE)

mx2 <- nb2mat(contig_nk,zero.policy = T)

#Nearest neighbours should produce all ones for row sums...? Tick!
#apply(mx2,1,sum)

write.csv(mx2,"StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_8nearestNeighb.csv")

#~~~~3-Census stuff~~~~----

#For now, going to repeat this whole section for the 5 census aggPCS processing
#Though at some point they could prob just be combined

#~~~~~~~~~~~~~~~~~~~~~~
# Put stitched data into sensible format(s) 3 census version----

#shpz <- list.files("StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",pattern="*.shp",recursive = T,full.names = T)
shpz <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw",pattern="*.shp",recursive = T,full.names = T)

test <- readShapeSpatial(shpz[1])#Why wasn't I using this before??
df <- data.frame(test)
write.csv(df,"")

#get name of file from directory; replace type with csv
#(Must be something simpler than this!)
filen <- function(fullFileName) {
  strsplit(fullFileName,"/") %>% 
    unlist %>% 
    tail(n=1) %>% 
    strsplit("\\.") %>% 
    unlist %>% 
    head(n=1) %>% 
    paste0(".csv")
}

filen(shpz[2])

#Load each, re-save as csv.
lapply(shpz,function(x) write.csv(data.frame(readShapeSpatial(x)),
       paste0("StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/",filen(x)), row.names=F))

#Some odd things there. Checking...
chk <- readShapeSpatial(shpz[3]) %>% data.frame

#Why are there two "middle "other middle east"s?
#Fixed. Dunno what happened there.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Few final 3-census geoff bits----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#A shapefile of the aggregated PCS zones
#Add: area of the zone (acres)
#Total pop per decade (actually total from CoB columns)
pcs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

pcs$area_metres <- gArea(pcs,byid = T)
pcs$area_acres <- pcs$area_metres / 4046.85642 

#load CoB decades to get zone total pop and attach. We have the names from above still
CoB91 <- readShapeSpatial(shpz[1]) %>% data.frame
CoB01 <- readShapeSpatial(shpz[2]) %>% data.frame
CoB11 <- readShapeSpatial(shpz[3]) %>% data.frame

CoB91$totalPop1991 <- apply(CoB91[,c(4:42)],1,sum)
CoB01$totalPop2001 <- apply(CoB01[,c(4:42)],1,sum)
CoB11$totalPop2011 <- apply(CoB11[,c(4:42)],1,sum)

pcs_df <- data.frame(pcs)

#Or I just could merge to make sure!
pcs@data <- merge(pcs@data,CoB91[,c('label','totalPop1991')], by = 'label')
pcs@data <- merge(pcs@data,CoB01[,c('label','totalPop2001')], by = 'label')
pcs@data <- merge(pcs@data,CoB11[,c('label','totalPop2011')], by = 'label')

#Check the populations look sensible
chk <- pcs@data[,c(1,5:7)] %>% 
  gather(yearz,count,totalPop1991:totalPop2011)

output <- ggplot(chk,aes(x = yearz, y = count, group = label)) +
  geom_line()
output

#Err.
output <- ggplot(chk,aes(x = yearz, y = count)) +
  geom_boxplot()
output

#Kind of! Need diff lags... but it'll probably do for now.
#Save the PCS with its extra bits
writeSpatialShape(pcs,'StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/postcodeSectors_plusArea_n_pops.shp')


#~~~~~~~~~~~~~~~~~~
#Combine census years together for the various files----
#~~~~~~~~~~~~~~~~~~

#Actually... let's just do this safely!
filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs",pattern="Country",recursive = T,full.names = T)

yearz <- c(1991,2001,2011)

CoB_combo <- do.call(rbind, lapply(c(1:3),
  function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
))

filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs",pattern="dwellings",recursive = T,full.names = T)

dwellings_combo <- do.call(rbind, lapply(c(1:3),
  function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
))

#EA has one column wrong, I think...
#And that now won't work. I must have been manually editing the CSVs before. Don't do that!
filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs",pattern="econActive",recursive = T,full.names = T)

#Load them, edit columns, get them in the right order. 
#Though order shouldn't matter if their content's correct?
ea91 <- read.csv(filez[1]) %>% mutate(censusYear = yearz[1])
ea01 <- read.csv(filez[2]) %>% mutate(censusYear = yearz[2])
ea11 <- read.csv(filez[3]) %>% mutate(censusYear = yearz[3])

names(ea91)
names(ea01)
names(ea11)

#1991 needs Umempl and EA reversing order
#Plus keep only necessary columns
ea91 <- ea91[,c(1,4,3,5,6)]
ea01 <- ea01[,c(2,4,5,6,7)]
ea11 <- ea11[,c(2,4,5,6,7)]

names(ea91)[names(ea91)=='prcntEm'] <- 'percentEmp'

#Combine straight from file
# EA_combo <- do.call(rbind, lapply(c(1:3),
#   function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
# ))

EA_combo <- do.call(rbind,list(ea91,ea01,ea11))

#Write!
write.csv(CoB_combo,'StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv', row.names = F)

#Wrong locations now...
write.csv(dwellings_combo,'file:///C:/Data/Census/StitchOutputs/Scotland/LBS_3censusCombinedData/dwellings.csv', row.names = F)
write.csv(EA_combo,'file:///C:/Data/Census/StitchOutputs/Scotland/LBS_3censusCombinedData/econActive.csv', row.names = F)

#~~~~~~~~~~~~~~~~
#Check dwelling zone change between censuses----
dwellingDiff <- data.frame(
  diff1 = dwellings_combo$totalDwell[dwellings_combo$censusYear==2001] - 
    dwellings_combo$totalDwell[dwellings_combo$censusYear==1991],
  diff2 = dwellings_combo$totalDwell[dwellings_combo$censusYear==2011] - 
    dwellings_combo$totalDwell[dwellings_combo$censusYear==2001]
  )

dwellingDiff2 <- dwellingDiff %>% gather(decade,diff)

ggplot(dwellingDiff2,aes(x=decade,y=diff)) +
  geom_boxplot()

#How many below zero?
table(dwellingDiff$diff1 < 0)
table(dwellingDiff$diff2 < 0)

#Do the zone names actually match?
#Yes
namez <- data.frame(
  ninetyOne = dwellings_combo$label[dwellings_combo$censusYear==1991],
  twoThousandOne = dwellings_combo$label[dwellings_combo$censusYear==2001],
  twoThousandEleven = dwellings_combo$label[dwellings_combo$censusYear==2011]
)


dwellingLookz <- data.frame(
  ninetyOne = dwellings_combo$totalDwell[dwellings_combo$censusYear==1991],
  twoThousandOne = dwellings_combo$totalDwell[dwellings_combo$censusYear==2001],
  twoThousandEleven = dwellings_combo$totalDwell[dwellings_combo$censusYear==2011]
)

apply(dwellingLookz,2,sum) %>% diff

#add zone codes to dwelling diffs for mapping
dwellingDiff$label <- namez$ninetyOne

#attach to spatial and save
#wellingDiffsp <- pcs
#Oops, wrong one!
pcs@data <- merge(pcs@data,dwellingDiff,by = 'label')

writeSpatialShape(pcs,'QGIS/temp/checkDwellinz.shp')

#~~~~~~~~~~~~~~~~
#Check total pop zone change (via tot CoB) between censuses----

#Using CoB_combo from above
#Drop channel islands for now, I accidentally double counted
CoB_combo2 <- CoB_combo %>% dplyr::select(-Channel_Is)

#Sum remaining CoBs as total zone pop
CoB_combo2 <- CoB_combo2 %>% mutate(totpop = rowSums(.[4:41]))

#Just keep totpop
CoB_combo2 <- CoB_combo2 %>% dplyr::select(1:2,42:43)

CoB_diff <- data.frame(
  diff1 = CoB_combo2$totpop[CoB_combo2$censusYear==2001] - 
    CoB_combo2$totpop[CoB_combo2$censusYear==1991],
  diff2 = CoB_combo2$totpop[CoB_combo2$censusYear==2011] - 
    CoB_combo2$totpop[CoB_combo2$censusYear==2001]
)

CoB_diff2 <- CoB_diff %>% gather(decade,diff)

ggplot(CoB_diff2,aes(x=decade,y=diff)) +
  geom_boxplot()

#OK, so a similar pattern to dwellings - which would make sense either due to problems
#with zone agg or because pop actually did drop.

#Assuming zone order stayed the same... Yup
table(CoB_combo$label == dwellings_combo$label)

#So do those zones correlate?
plot(dwellingDiff$diff1,CoB_diff$diff1)
plot(dwellingDiff$diff2,CoB_diff$diff2)

#My spidey sense is those are (mostly) correct. The way they change between censuses...
#But that could just be census change biasing the underlying mistake with something that
#makes it look plausible.

#~~~~~~~~~~~~~~~~
#Check econ active zone change between censuses----

#In theory, if there's the same type of change, that suggests zone summing is causing it
EA_perc_diff <- data.frame(
  diff1 = EA_combo$percentEmp[EA_combo$censusYear==2001] - 
    EA_combo$percentEmp[EA_combo$censusYear==1991],
  diff2 = EA_combo$percentEmp[EA_combo$censusYear==2011] - 
    EA_combo$percentEmp[EA_combo$censusYear==2001]
)

EA_diff2 <- EA_perc_diff %>% gather(decade,diff)

ggplot(EA_diff2,aes(x=decade,y=diff)) +
  geom_boxplot()

#Ah ha! Phew. Looks promising.
plot(dwellingDiff$diff1,EA_perc_diff$diff1)
plot(dwellingDiff$diff2,EA_perc_diff$diff2)


#~~~~5-Census stuff~~~~----

#A copy of above for 5 census aggPCS
#Where a lot of the data has been copied from 3 census
#Where it was available

#~~~~~~~~~~~~~~~~~~~~~~
#Put stitched data into sensible format(s) 5 census version----

shpz <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw",pattern="*.shp",recursive = T,full.names = T)

test <- readShapeSpatial(shpz[1])#Why wasn't I using this before??
df <- data.frame(test)
#write.csv(df,"")

#get name of file from directory; replace type with csv
#(Must be something simpler than this!)
filen <- function(fullFileName) {
  strsplit(fullFileName,"/") %>% 
    unlist %>% 
    tail(n=1) %>% 
    strsplit("\\.") %>% 
    unlist %>% 
    head(n=1) %>% 
    paste0(".csv")
}

filen(shpz[2])

#Load each, re-save as csv.
lapply(shpz,function(x) write.csv(data.frame(readShapeSpatial(x)),
                                  paste0("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/",filen(x)), row.names=F))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5-census geoff bits----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#A shapefile of the aggregated PCS zones
#Add: area of the zone (acres)
#Total pop per decade (actually total from CoB columns)
pcs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

pcs$area_metres <- gArea(pcs,byid = T)
pcs$area_acres <- pcs$area_metres / 4046.85642 

#load CoB decades to get zone total pop and attach. We have the names from above still
CoB71 <- readShapeSpatial(shpz[1]) %>% data.frame
CoB81 <- readShapeSpatial(shpz[2]) %>% data.frame
CoB91 <- readShapeSpatial(shpz[3]) %>% data.frame
CoB01 <- readShapeSpatial(shpz[4]) %>% data.frame
CoB11 <- readShapeSpatial(shpz[5]) %>% data.frame

CoB71$totalPop1971 <- apply(CoB71[,c(3:16)],1,sum)
CoB81$totalPop1981 <- apply(CoB81[,c(3:16)],1,sum)
CoB91$totalPop1991 <- apply(CoB91[,c(4:17)],1,sum)#The original data not summed to zones
CoB01$totalPop2001 <- apply(CoB01[,c(3:16)],1,sum)
CoB11$totalPop2011 <- apply(CoB11[,c(3:16)],1,sum)

pcs_df <- data.frame(pcs)

#Or I just could merge to make sure!
pcs@data <- merge(pcs@data,CoB71[,c('label','totalPop1971')], by = 'label')
pcs@data <- merge(pcs@data,CoB81[,c('label','totalPop1981')], by = 'label')
pcs@data <- merge(pcs@data,CoB91[,c('label','totalPop1991')], by = 'label')
pcs@data <- merge(pcs@data,CoB01[,c('label','totalPop2001')], by = 'label')
pcs@data <- merge(pcs@data,CoB11[,c('label','totalPop2011')], by = 'label')

#Check the populations look sensible
chk <- pcs@data[,c(1,5:9)] %>% 
  gather(yearz,count,totalPop1971:totalPop2011)

output <- ggplot(chk,aes(x = yearz, y = count, group = label)) +
  geom_line()
output

#Err.
output <- ggplot(chk,aes(x = yearz, y = count)) +
  geom_boxplot()
output

#Kind of! Need diff lags... but it'll probably do for now.
#Save the PCS with its extra bits
# writeSpatialShape(pcs,'StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/postcodeSectors_plusArea_n_pops.shp')
write_csv(pcs@data,'StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/postcodeSectors_plusArea_n_pops.csv')



#~~~~~~~~~~~~~~~~~~
#Combine census years together for the various files----
#~~~~~~~~~~~~~~~~~~

#Actually... let's just do this safely!
filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs",pattern="Country|CoB",recursive = T,full.names = T)

yearz <- c(1971,1981,1991,2001,2011)

#Slight mismatch in names / column number, needs doing manually.
# CoB_combo <- do.call(rbind, lapply(c(1:5),
#   function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
# ))
CoBs <- lapply(c(1:5),
  function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
)

#Tweak 1991
CoBs[[3]] <- CoBs[[3]][,c(2:18)]
names(CoBs[[3]]) <- names(CoBs[[1]])#All others are the same

CoB_combo <- do.call(rbind, CoBs)


#Don't do dwellings yet until we know what we're doing with the housing data for 5-census
# filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs",pattern="dwellings",recursive = T,full.names = T)
# 
# dwellings_combo <- do.call(rbind, lapply(c(1:3),
#   function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
# ))

#EA has one column wrong, I think...
#And that now won't work. I must have been manually editing the CSVs before. Don't do that!
filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs",pattern="econActive",recursive = T,full.names = T)

#Load them, edit columns, get them in the right order. 
#Though order shouldn't matter if their content's correct?
ea71 <- read.csv(filez[1]) %>% mutate(censusYear = yearz[1])
ea81 <- read.csv(filez[2]) %>% mutate(censusYear = yearz[2])
ea91 <- read.csv(filez[3]) %>% mutate(censusYear = yearz[3])
ea01 <- read.csv(filez[4]) %>% mutate(censusYear = yearz[4])
ea11 <- read.csv(filez[5]) %>% mutate(censusYear = yearz[5])

names(ea71)
names(ea81)
names(ea91)
names(ea01)
names(ea11)

#1991 needs Umempl and EA reversing order
#Plus keep only necessary columns
ea71 <- ea71[,c(1,4,3,5,6)]
ea81 <- ea81[,c(1,3:6)]
ea91 <- ea91[,c(1,4,3,5,6)]
ea01 <- ea01[,c(2,4:7)]
ea11 <- ea11[,c(2,4:7)]

names(ea71)[names(ea71)=='prcntEm'] <- 'percentEmp'
names(ea81)[names(ea81)=='prcntEm'] <- 'percentEmp'
names(ea91)[names(ea91)=='prcntEm'] <- 'percentEmp'

#Combine straight from file
# EA_combo <- do.call(rbind, lapply(c(1:3),
#   function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
# ))

EA_combo <- do.call(rbind,list(ea71,ea81,ea91,ea01,ea11))

#give the CoB_combo data some better CoB names
#get names from category stitch
names71 <- read_csv('VariableCoding/CountryOfBirth_fiveCensusRecodes/71.csv')

#match?
names(names71)[2:15]
names(CoB_combo)[3:16]

names(CoB_combo)[3:16] <- gsub(" ", "", names(names71)[2:15]) 

names(CoB_combo)[3:14] <- gsub("NewC", "(New-C)", names(CoB_combo)[3:14]) 


#Write!
write_csv(CoB_combo,'StitchOutputs/Scotland/LBS_5censusCombinedData/countryOfBirth.csv')
# write.csv(dwellings_combo,'file:///C:/Data/Census/StitchOutputs/Scotland/LBS_3censusCombinedData/dwellings.csv', row.names = F)
write.csv(EA_combo,'StitchOutputs/Scotland/LBS_5censusCombinedData/econActive.csv', row.names = F)

#~~~~~~~~~~~~~~~~
#Check dwelling zone change between censuses----
# dwellingDiff <- data.frame(
#   diff1 = dwellings_combo$totalDwell[dwellings_combo$censusYear==2001] - 
#     dwellings_combo$totalDwell[dwellings_combo$censusYear==1991],
#   diff2 = dwellings_combo$totalDwell[dwellings_combo$censusYear==2011] - 
#     dwellings_combo$totalDwell[dwellings_combo$censusYear==2001]
#   )
# 
# dwellingDiff2 <- dwellingDiff %>% gather(decade,diff)
# 
# ggplot(dwellingDiff2,aes(x=decade,y=diff)) +
#   geom_boxplot()
# 
# #How many below zero?
# table(dwellingDiff$diff1 < 0)
# table(dwellingDiff$diff2 < 0)
# 
# #Do the zone names actually match?
# #Yes
# namez <- data.frame(
#   ninetyOne = dwellings_combo$label[dwellings_combo$censusYear==1991],
#   twoThousandOne = dwellings_combo$label[dwellings_combo$censusYear==2001],
#   twoThousandEleven = dwellings_combo$label[dwellings_combo$censusYear==2011]
# )
# 
# 
# dwellingLookz <- data.frame(
#   ninetyOne = dwellings_combo$totalDwell[dwellings_combo$censusYear==1991],
#   twoThousandOne = dwellings_combo$totalDwell[dwellings_combo$censusYear==2001],
#   twoThousandEleven = dwellings_combo$totalDwell[dwellings_combo$censusYear==2011]
# )
# 
# apply(dwellingLookz,2,sum) %>% diff
# 
# #add zone codes to dwelling diffs for mapping
# dwellingDiff$label <- namez$ninetyOne
# 
# #attach to spatial and save
# #wellingDiffsp <- pcs
# #Oops, wrong one!
# pcs@data <- merge(pcs@data,dwellingDiff,by = 'label')
# 
# writeSpatialShape(pcs,'QGIS/temp/checkDwellinz.shp')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~BACK TO OTHER STUFF~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Intersect PCS-aggs with 2-fold rural/urban classification----

#Trying the raster methods here:
#http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r

ur <- readOGR(dsn = 'C:/Data/MapPolygons/Scotland/urbanRural', layer = '2foldDissolveScotsUrbanRural')

pcs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

#intz <- intersect(pcs,ur)

#That's way too slow. Let's try it on each individual PCS zone one at a time
#And use only urban - can deduce others from that.
urb <- ur[1,]

# getz <- 284
# test <- intersect(pcs[getz,],urb)
# 
# #self-intersections being problem apparently.
# for(getz in c(283:822)){
# 
# test <- intersect(pcs[getz,],urb)
# 
#   if(!is.null(test)){
#     test@data
#   }
# 
# print(getz)
# 
# # plot(test)
# # plot(pcs[getz,], add = T)
# }

#Try Gintersection instead
getz <- 284

test <- gIntersection(pcs[getz,],urb)
plot(pcs[getz,])
plot(test, add=T, col='RED')

gArea(pcs[getz,])
gArea(test)

#Is more than 0.5 so I'd class as urban (intersected with urban, recall.)
gArea(test)/gArea(pcs[getz,])

#OK, so: can also use the following to do basic "does it intersect" test
gIntersects(pcs[1,],urb)
gIntersects(pcs[284,],urb)

#Quick mark of which PCSs intersect urban at all
pcs@data$intersectsUrban <- sapply(c(1:822), function(x) gIntersects(pcs[x,],urb))
table(pcs@data$intersectsUrban)

#For those that do, find out how much

#Index of those that do intersect
indexOfIntersects <- which(pcs@data$intersectsUrban, arr.ind = T)

#Set all to zero
pcs@data$intersectUrbanRatio <- 0

#Overwrite trues with ratio
# pcs@data$intersectUrbanRatio[pcs@data$intersectsUrban] <- 
#   sapply(indexOfIntersects, function(x) {
#     gArea( gIntersection(pcs[x,],urb) ) / gArea(pcs[x,] )
#   })

#Let's do with for-loop so's we can see woss appnin.
for(x in indexOfIntersects) {
  
  pcs@data$intersectUrbanRatio[x] <- gArea( gIntersection(pcs[x,],urb) ) / gArea(pcs[x,] )
  
  print(x)
  
}

#save those values somewhere.
#So reminder: intersectUrbanRatio is the amount this zone overlaps with the urban shapefile
#So in theory we could use different ratios of overlap...
write_csv(pcs@data, 'R_data/pcsAggregatedZeroCounts_urbanRatios.csv')
















