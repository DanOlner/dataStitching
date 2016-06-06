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

plot(iz, col='grey')
plot(contig_nk,coords,col='red',lwd=2,add=TRUE)

mx2 <- nb2mat(contig_nk,zero.policy = T)

write.csv(mx2,"StitchOutputs/Scotland/other/Scots_IZs_spatialWeights_4nearestNeighb.csv")

#~~~~~~~~~~~~~~~~~~~~~~
# Put stitched data into sensible format(s)

shpz <- list.files("StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",pattern="*.shp",recursive = T,full.names = T)

test <- readShapeSpatial(shpz[1])#Why wasn't I using this before??
df <- data.frame(test)
write.csv(df,"")

#get name of file from directory; replace type with csv
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
       paste0("StitchOutputs/Scotland/fiveCensus_to2011_IZs_csvs/",filen(x)), row.names=F))




















