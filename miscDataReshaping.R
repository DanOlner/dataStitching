#We only have back to 1990, so can only cover 3 censuses
geolibs <- c("spdep","dplyr", "tidyr","assertthat","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, require, character.only = TRUE)

#general data reshaping and saving in different forms for different people at different times.

#5 census stitching: save as CSVs for Geoff.

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

contig_nk <- knn2nb(knearneigh(coords,k=24), row.names = iz@data$label)


contig_nk_df <- contig_nk %>% data.frame()

#plot(iz, col='grey')
#xy=locator(2,"p") 
# plot(scotmap, xlim=xy$x,ylim=xy$y)
#plot(iz, col='grey', xlim=xy$x,ylim=xy$y)
#plot(contig_nk,coords,col='red',lwd=2,add=TRUE)

mx2 <- nb2mat(contig_nk,zero.policy = T)

#Nearest neighbours should produce all ones for row sums...? Tick!
#apply(mx2,1,sum)

write.csv(mx2,"StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_24nearestNeighb.csv")


#~~~~~~~~~~~~~~~~~~~~~~
# Put stitched data into sensible format(s)----

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
#Few final 3-census geoff bits
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
#Combine census years together for the various files
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

filez <- list.files("StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs",pattern="econActive",recursive = T,full.names = T)

EA_combo <- do.call(rbind, lapply(c(1:3),
  function(x) read.csv(filez[x]) %>% mutate(censusYear = yearz[x])
))

#Write!
write.csv(CoB_combo,'file:///C:/Data/Census/StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv', row.names = F)
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







